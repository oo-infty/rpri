use std::fmt::{Display, Formatter, Result as FmtResult};

use enum_dispatch::enum_dispatch;
use snafu::prelude::*;

use crate::entity::base::EntityId;
use crate::entity::expression::{Argument, Expression, ExpressionNode, ExpressionView};

use super::base::VariableIdentifier;

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
    pub fn try_new(expr: ExpressionNode, entity_id: EntityId) -> Result<Self, TryNewFactError> {
        let view = expr.view(false).as_predicate();
        ensure!(view.is_some(), NotPredicateF { expr });
        ensure!(view.is_some_and(|(_, negated)| !negated), NegatedF { expr });
        Ok(Self { expr, entity_id })
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

#[derive(Debug, Snafu, PartialEq, Eq)]
#[snafu(context(suffix(F)))]
pub enum TryNewFactError {
    #[snafu(display("A fact should be a predicate. Got: `{expr}`."))]
    NotPredicate { expr: ExpressionNode },
    #[snafu(display("A fact should not be a negated predicate. Got: `{expr}`."))]
    Negated { expr: ExpressionNode },
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
    ) -> Result<Self, TryNewRuleError> {
        let view = head.view(false).as_predicate();
        ensure!(view.is_some(), HeadNotPredicateR { head });
        ensure!(
            view.is_some_and(|(_, negated)| !negated),
            NegatedHeadR { head }
        );

        let variable = head
            .arguments()
            .iter()
            .filter_map(|arg| match arg {
                Argument::Variable(v) => Some(v),
                _ => None,
            })
            .filter(|v| v.inner().starts_with(|c: char| c.is_ascii_uppercase()))
            .map(|v| Argument::Variable(v.clone()))
            .find(|v| body.arguments().iter().all(|v2| v != v2));

        ensure!(
            variable.is_none(),
            FreeArgumentR {
                head,
                body,
                variable: match variable {
                    Some(Argument::Variable(v)) => v,
                    _ => unreachable!(),
                }
            }
        );

        Ok(Self {
            head,
            body,
            entity_id,
        })
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

#[derive(Debug, Snafu, PartialEq, Eq)]
#[snafu(context(suffix(R)))]
pub enum TryNewRuleError {
    #[snafu(display("The head of a rule should be a predicate. Got: `{head}`."))]
    HeadNotPredicate { head: ExpressionNode },
    #[snafu(display("The head of a rule should not be a negated predicate. Got: `{head}`."))]
    NegatedHead { head: ExpressionNode },
    #[snafu(display("`variable` in `head` is not constrained by `body`. Ignore this by adding a `_` explicitly."))]
    FreeArgument {
        head: ExpressionNode,
        body: ExpressionNode,
        variable: VariableIdentifier,
    },
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::entity::expression::{ExpressionElement, ExpressionKind};
    use crate::entity::predicate::{PredicateDefinition, PredicateHandle, Signature};

    #[test]
    fn fact_build() {
        let expr = make_predicate_expr(false);
        let fact = Fact::try_new(expr, 0).unwrap();
        assert_eq!(
            fact.arguments(),
            &vec![Argument::Variable("X".parse().unwrap())]
        );

        let expr = make_conjunction_expr();
        let fact = Fact::try_new(expr, 1);
        assert!(matches!(
            fact,
            Err(TryNewFactError::NotPredicate { expr: _ })
        ));

        let expr = make_predicate_expr(true);
        let fact = Fact::try_new(expr, 0);
        assert!(matches!(fact, Err(TryNewFactError::Negated { expr: _ })));
    }

    #[test]
    fn rule_build() {
        let head = make_predicate_expr(false);
        let body = make_conjunction_expr();
        let rule = Rule::try_new(head, body, 0).unwrap();
        assert_eq!(
            rule.arguments(),
            &vec![Argument::Variable("X".parse().unwrap())]
        );
        assert_eq!(rule.body().unwrap().to_string(), r"pred(X), \+pred(X)");

        let head = make_conjunction_expr();
        let body = make_conjunction_expr();
        let rule = Rule::try_new(head, body, 0);
        assert!(matches!(
            rule,
            Err(TryNewRuleError::HeadNotPredicate { head: _ })
        ));

        let head = make_predicate_expr(true);
        let body = make_conjunction_expr();
        let rule = Rule::try_new(head, body, 0);
        assert!(matches!(
            rule,
            Err(TryNewRuleError::NegatedHead { head: _ })
        ));
    }

    #[test]
    fn rule_build_free_argument_err() {
        let head = make_predicate_expr(false);

        let signature = Signature::new("pred".parse().unwrap(), 1);
        let predicate = PredicateDefinition::new(signature, 0);
        let predicate = PredicateHandle::from(predicate);
        let body = ExpressionNode {
            arguments: vec![Argument::Variable("Y".parse().unwrap())],
            elements: ExpressionElement::Predicate(predicate),
            kind: ExpressionKind::Conjunctive,
            negated: false,
            entity_id: 0,
        };

        let rule = Rule::try_new(head, body, 0);

        match rule {
            Err(TryNewRuleError::FreeArgument {
                head: _,
                body: _,
                variable,
            }) => assert_eq!(variable, "X".parse::<VariableIdentifier>().unwrap()),
            _ => unreachable!(),
        }
    }

    #[test]
    fn clause_display() {
        let expr = make_predicate_expr(false);
        let fact = Fact::try_new(expr, 0).unwrap();
        assert_eq!(fact.to_string(), "pred(X).");

        let head = make_predicate_expr(false);
        let body = make_conjunction_expr();
        let rule = Rule::try_new(head, body, 0).unwrap();
        assert_eq!(rule.to_string(), r"pred(X) :- pred(X), \+pred(X).");
    }

    fn make_predicate_expr(negated: bool) -> ExpressionNode {
        let signature = Signature::new("pred".parse().unwrap(), 1);
        let predicate = PredicateDefinition::new(signature, 0);
        let predicate = PredicateHandle::from(predicate);

        ExpressionNode {
            arguments: vec![Argument::Variable("X".parse().unwrap())],
            elements: ExpressionElement::Predicate(predicate),
            kind: ExpressionKind::Conjunctive,
            negated,
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
