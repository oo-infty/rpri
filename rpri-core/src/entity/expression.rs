use std::fmt::{Display, Formatter, Result as FmtResult};
use std::ops::BitXor;
use std::slice::Iter;

use enum_dispatch::enum_dispatch;

use crate::entity::atom::AtomHandle;
use crate::entity::base::{EntityId, VariableIdentifier};
use crate::entity::predicate::{Predicate, PredicateHandle};

/// Argument of predicates, which can be a variable or constant.
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
    fn id(&self) -> EntityId;

    fn arguments(&self) -> &Vec<Argument>;

    fn kind(&self) -> ExpressionKind;

    fn view(&self, negated: bool) -> ExpressionView;

    fn iter(&self) -> ExpressionIter;
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
#[derive(Debug, Clone)]
pub struct ExpressionNode {
    pub(super) arguments: Vec<Argument>,
    pub(super) elements: ExpressionElement,
    pub(super) kind: ExpressionKind,
    pub(super) negated: bool,
    pub(super) entity_id: EntityId,
}

impl Expression for ExpressionNode {
    fn id(&self) -> EntityId {
        self.entity_id
    }

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

    fn iter(&self) -> ExpressionIter {
        match self.view(false) {
            ExpressionView::Predicate(view) => ExpressionIter::Single {
                inner: self,
                negated: view.negated,
            },
            ExpressionView::Conjunction(view) => match &self.elements {
                ExpressionElement::SubExpressions(expr) => ExpressionIter::Sequence {
                    iter: expr.iter(),
                    negate_each: view.negate_each,
                },
                _ => unreachable!(),
            },
            ExpressionView::Disjunction(view) => match &self.elements {
                ExpressionElement::SubExpressions(expr) => ExpressionIter::Sequence {
                    iter: expr.iter(),
                    negate_each: view.negate_each,
                },
                _ => unreachable!(),
            },
        }
    }
}

impl PartialEq for ExpressionNode {
    fn eq(&self, other: &Self) -> bool {
        if self.entity_id == other.entity_id {
            debug_assert_eq!(self.arguments, other.arguments);
            debug_assert_eq!(self.elements, other.elements);
            debug_assert_eq!(self.kind, other.kind);
            debug_assert_eq!(self.negated, other.negated);
        }
        self.entity_id == other.entity_id
    }
}

impl Eq for ExpressionNode {}

impl Display for ExpressionNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self.view(false) {
            ExpressionView::Predicate(s) => s.fmt(f),
            ExpressionView::Conjunction(s) => s.fmt(f),
            ExpressionView::Disjunction(s) => s.fmt(f),
        }
    }
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

#[derive(Debug, Clone)]
pub struct PredicateView<'a> {
    target: &'a ExpressionNode,
    negated: bool,
}

impl Expression for PredicateView<'_> {
    fn id(&self) -> EntityId {
        self.target.id()
    }

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

    fn iter(&self) -> ExpressionIter {
        ExpressionIter::Single {
            inner: self.target,
            negated: self.negated,
        }
    }
}

impl PartialEq for PredicateView<'_> {
    fn eq(&self, other: &Self) -> bool {
        if self.id() == other.id() {
            debug_assert_eq!(self.target, other.target);
            debug_assert_eq!(self.negated, other.negated);
        }
        self.id() == other.id()
    }
}

impl Eq for PredicateView<'_> {}

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

#[derive(Debug, Clone)]
pub struct ConjunctionView<'a> {
    target: &'a ExpressionNode,
    negate_each: bool,
}

impl Expression for ConjunctionView<'_> {
    fn id(&self) -> EntityId {
        self.target.id()
    }

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

    fn iter(&self) -> ExpressionIter {
        match &self.target.elements {
            ExpressionElement::SubExpressions(expr) => ExpressionIter::Sequence {
                iter: expr.iter(),
                negate_each: self.negate_each,
            },
            _ => unreachable!(),
        }
    }
}

impl PartialEq for ConjunctionView<'_> {
    fn eq(&self, other: &Self) -> bool {
        if self.id() == other.id() {
            debug_assert_eq!(self.target, other.target);
            debug_assert_eq!(self.negate_each, other.negate_each);
        }
        self.id() == other.id()
    }
}

impl Eq for ConjunctionView<'_> {}

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

#[derive(Debug, Clone)]
pub struct DisjunctionView<'a> {
    target: &'a ExpressionNode,
    negate_each: bool,
}

impl Expression for DisjunctionView<'_> {
    fn id(&self) -> EntityId {
        self.target.id()
    }

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

    fn iter(&self) -> ExpressionIter {
        match &self.target.elements {
            ExpressionElement::SubExpressions(expr) => ExpressionIter::Sequence {
                iter: expr.iter(),
                negate_each: self.negate_each,
            },
            _ => unreachable!(),
        }
    }
}

impl PartialEq for DisjunctionView<'_> {
    fn eq(&self, other: &Self) -> bool {
        if self.id() == other.id() {
            debug_assert_eq!(self.target, other.target);
            debug_assert_eq!(self.negate_each, other.negate_each);
        }
        self.id() == other.id()
    }
}

impl Eq for DisjunctionView<'_> {}

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

pub enum ExpressionIter<'a> {
    Single {
        inner: &'a ExpressionNode,
        negated: bool,
    },
    Sequence {
        iter: Iter<'a, ExpressionNode>,
        negate_each: bool,
    },
    None,
}

impl<'a> Iterator for ExpressionIter<'a> {
    type Item = ExpressionView<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            ExpressionIter::Single { inner, negated } => {
                let res = inner.view(inner.negated.bitxor(*negated));
                *self = ExpressionIter::None;
                Some(res)
            }
            ExpressionIter::Sequence { iter, negate_each } => {
                iter.next().map(|expr| expr.view(*negate_each))
            }
            ExpressionIter::None => None,
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

        ExpressionNode {
            arguments: if has_arg {
                vec![Argument::Variable("X".parse().unwrap())]
            } else {
                vec![]
            },
            elements: ExpressionElement::Predicate(predicate),
            kind: ExpressionKind::Conjunctive,
            negated: false,
            entity_id: 0,
        }
    }

    fn make_conjunction_expr() -> ExpressionNode {
        ExpressionNode {
            arguments: vec![Argument::Variable("X".parse().unwrap())],
            elements: ExpressionElement::SubExpressions(vec![
                make_predicate_expr(false),
                make_predicate_expr(true),
            ]),
            kind: ExpressionKind::Conjunctive,
            negated: false,
            entity_id: 0,
        }
    }

    fn make_combined_expr() -> ExpressionNode {
        ExpressionNode {
            arguments: vec![Argument::Variable("X".parse().unwrap())],
            elements: ExpressionElement::SubExpressions(vec![
                make_predicate_expr(false),
                make_conjunction_expr(),
                make_predicate_expr(true),
            ]),
            kind: ExpressionKind::Disjunctive,
            negated: false,
            entity_id: 0,
        }
    }
}
