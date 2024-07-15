use std::fmt::{Display, Formatter, Result as FmtResult};

use enum_dispatch::enum_dispatch;

use crate::entity::base::EntityId;
use crate::entity::expression::{Argument, Expression, ExpressionNode, ExpressionView};

/// An abstract form of representation of knowledge in Prolog's DB, which
/// consists of facts and rules.
#[enum_dispatch]
pub trait Clause: Clone + PartialEq + Eq + Display {
    fn id(&self) -> EntityId;

    fn arguments(&self) -> &Vec<Argument>;

    fn body(&self) -> Option<ExpressionView>;
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[enum_dispatch(Clause)]
pub enum ConcreteClause {
    Fact(Fact),
    Rule(Rule),
}

impl Display for ConcreteClause {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            ConcreteClause::Fact(s) => s.fmt(f),
            ConcreteClause::Rule(s) => s.fmt(f),
        }
    }
}

/// Type that consists of only one expression of a predicate.
#[derive(Debug, Clone)]
pub struct Fact {
    expr: ExpressionNode,
    entity_id: EntityId,
}

impl Fact {
    pub fn try_new(expr: ExpressionNode, entity_id: EntityId) -> Result<Self, ()> {
        if expr.view(false).as_predicate().is_some() {
            Ok(Self { expr, entity_id })
        } else {
            Err(())
        }
    }
}

impl Clause for Fact {
    fn id(&self) -> EntityId {
        self.entity_id
    }

    fn arguments(&self) -> &Vec<Argument> {
        self.expr.arguments()
    }

    fn body(&self) -> Option<ExpressionView> {
        None
    }
}

impl PartialEq for Fact {
    fn eq(&self, other: &Self) -> bool {
        if self.entity_id == other.entity_id {
            debug_assert_eq!(self.expr, other.expr);
        }
        self.entity_id == other.entity_id
    }
}

impl Eq for Fact {}

impl Display for Fact {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{}.", self.expr)
    }
}

/// Type that consists of one conclusion (head) and one expression of several
/// premises (body).
#[derive(Debug, Clone)]
pub struct Rule {
    head: ExpressionNode,
    body: ExpressionNode,
    entity_id: EntityId,
}

impl Rule {
    pub fn try_new(
        head: ExpressionNode,
        body: ExpressionNode,
        entity_id: EntityId,
    ) -> Result<Self, ()> {
        if head.view(false).as_predicate().is_some() {
            Ok(Self {
                head,
                body,
                entity_id,
            })
        } else {
            Err(())
        }
    }
}

impl Clause for Rule {
    fn id(&self) -> EntityId {
        self.entity_id
    }

    fn arguments(&self) -> &Vec<Argument> {
        self.head.arguments()
    }

    fn body(&self) -> Option<ExpressionView> {
        Some(self.body.view(false))
    }
}

impl PartialEq for Rule {
    fn eq(&self, other: &Self) -> bool {
        if self.entity_id == other.entity_id {
            debug_assert_eq!(self.head, other.head);
            debug_assert_eq!(self.body, other.body);
        }
        self.entity_id == other.entity_id
    }
}

impl Eq for Rule {}

impl Display for Rule {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{} :- {}.", self.head, self.body)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::entity::expression::{ExpressionElement, ExpressionKind};
    use crate::entity::predicate::{PredicateDefinition, PredicateHandle, Signature};

    #[test]
    fn fact_build() {
        let expr = make_predicate_expr(true);
        let fact = Fact::try_new(expr, 0).unwrap();
        assert_eq!(
            fact.arguments(),
            &vec![Argument::Variable("X".parse().unwrap())]
        );

        let expr = make_conjunction_expr();
        let fact = Fact::try_new(expr, 1);
        assert_eq!(fact, Err(()));
    }

    #[test]
    fn rule_build() {
        let head = make_predicate_expr(true);
        let body = make_conjunction_expr();
        let rule = Rule::try_new(head, body, 0).unwrap();
        assert_eq!(
            rule.arguments(),
            &vec![Argument::Variable("X".parse().unwrap())]
        );
        assert_eq!(rule.body().unwrap().to_string(), "pred, pred(X)");
    }

    #[test]
    fn clause_display() {
        let expr = make_predicate_expr(false);
        let fact = Fact::try_new(expr, 0).unwrap();
        assert_eq!(fact.to_string(), "pred.");

        let head = make_predicate_expr(true);
        let body = make_conjunction_expr();
        let rule = Rule::try_new(head, body, 0).unwrap();
        assert_eq!(rule.to_string(), "pred(X) :- pred, pred(X).");
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
}
