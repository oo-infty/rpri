use std::collections::{HashMap, HashSet};

use snafu::prelude::*;

use crate::entity::atom::{AtomDefinition, AtomHandle};
use crate::entity::base::{EntityId, Identifier};
use crate::entity::clause::ConcreteClause;
use crate::entity::predicate::{Predicate, PredicateDefinition, PredicateHandle, Signature};

/// A type that stores all atoms and manages their `EntityId`s.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct AtomState {
    atoms: Vec<AtomHandle>,
    atom_id: HashMap<Identifier, EntityId>,
}

impl AtomState {
    pub fn new() -> Self {
        Default::default()
    }

    /// Insert an atom named `identifier` and return its handle. If it already
    /// exists, return `InsertError`.
    pub fn insert(&mut self, identifier: Identifier) -> Result<AtomHandle, InsertError> {
        match self.atom_id.get(&identifier) {
            Some(_) => Err(InsertError::Duplicated),
            None => {
                let entity_id = self.atoms.len();
                let atom = AtomDefinition::new(identifier.clone(), entity_id);
                let atom = AtomHandle::from(atom);
                self.atoms.push(atom.clone());
                self.atom_id.insert(identifier, entity_id);
                Ok(atom)
            }
        }
    }

    /// Get the handle of an atom named `identifier`.
    pub fn get(&mut self, identifier: &Identifier) -> Option<&AtomHandle> {
        self.atom_id
            .get(identifier)
            .and_then(|&entity_id| self.atoms.get(entity_id))
    }
}

/// A type that stores all predicates and their relavent data and manages their
/// `EntityId`s.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct PredicateState {
    predicates: Vec<PredicateHandle>,
    predicate_id: HashMap<Identifier, EntityId>,
    domains: Vec<PredicateDomain>,
}

impl PredicateState {
    pub fn new() -> Self {
        Default::default()
    }

    /// Insert an atom named `identifier` and return its handle. If it already
    /// exists, return `InsertError`.
    pub fn insert(
        &mut self,
        identifier: Identifier,
        arity: usize,
    ) -> Result<PredicateHandle, InsertError> {
        match self.predicate_id.get(&identifier) {
            Some(&entity_id) => {
                let Some(predicate) = self.predicates.get(entity_id) else {
                    unreachable!()
                };

                if predicate.signature().arity() == arity {
                    Err(InsertError::Duplicated)
                } else {
                    Err(InsertError::Conflict)
                }
            }
            None => {
                let signature = Signature::new(identifier.clone(), arity);
                let entity_id = self.predicates.len();
                let predicate = PredicateDefinition::new(signature, entity_id);
                let predicate = PredicateHandle::from(predicate);
                self.predicates.push(predicate.clone());
                self.predicate_id.insert(identifier, entity_id);
                self.domains.push(PredicateDomain::new(arity));
                Ok(predicate)
            }
        }
    }

    /// Get the handle of an predicate named `identifier`. Return `None` if no
    /// key matches.
    pub fn get(&mut self, identifier: &Identifier) -> Option<&PredicateHandle> {
        self.predicate_id
            .get(identifier)
            .and_then(|&entity_id| self.predicates.get(entity_id))
    }

    /// Insert `atom` to `predicate`'s domain of `pos`-th argument and return
    /// true. Return false if `predicate` doesn't exist or `pos` is out of
    /// bounds.
    pub fn domain_insert(
        &mut self,
        predicate: &PredicateHandle,
        pos: usize,
        atom: AtomHandle,
    ) -> bool {
        self.domains
            .get_mut(predicate.id())
            .map(|domain| domain.insert(pos, atom))
            .unwrap_or(false)
    }

    /// Get `predicate`'s domain. Return `None` if `predicate` doesn't exist.
    pub fn domain(&self, predicate: &PredicateHandle) -> Option<&PredicateDomain> {
        self.domains.get(predicate.id())
    }
}

#[derive(Debug, Clone, Snafu, PartialEq, Eq)]
pub enum InsertError {
    #[snafu(display("Value indexed by the same identifier already exists, but it's equal to the one to be inserted."))]
    Duplicated,
    #[snafu(display("Value indexed by the same identifier already exists, along with conflicts in other fields."))]
    Conflict,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct PredicateDomain {
    values: Vec<HashSet<AtomHandle>>,
}

/// A type that represents sets of possible values that a predicate can accept
/// with respect its arguments.
impl PredicateDomain {
    fn new(arity: usize) -> Self {
        let mut values = Vec::with_capacity(arity);
        values.resize_with(arity, || HashSet::<_>::new());
        Self { values }
    }

    /// Insert an atom to the `pos`-th argument's set and return `true`. Return
    /// `false` if out of bounds.
    pub fn insert(&mut self, pos: usize, value: AtomHandle) -> bool {
        let Some(values) = self.values.get_mut(pos) else {
            return false;
        };
        values.insert(value);
        true
    }

    /// Get the set corresponding to the `pos`-th argument. Return `None` if out
    /// of bounds.
    pub fn get(&self, pos: usize) -> Option<&HashSet<AtomHandle>> {
        self.values.get(pos)
    }
}

/// A type that stores all clauses and groups them by predicates.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ClauseState {
    clauses: Vec<Vec<ConcreteClause>>,
}

impl ClauseState {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn insert(&mut self, predicate: &PredicateHandle, clause: ConcreteClause) {
        let entity_id = predicate.id();

        if entity_id >= self.clauses.len() {
            let additional = 2 * (entity_id + 1) - self.clauses.len();
            self.clauses.reserve(additional);
            self.clauses.resize_with(entity_id + 1, || Vec::new());
        }

        match self.clauses.get_mut(entity_id) {
            Some(clauses) => clauses.push(clause),
            None => unreachable!(),
        }
    }

    pub fn get(&self, predicate: &PredicateHandle) -> Option<&Vec<ConcreteClause>> {
        self.clauses.get(predicate.id())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::entity::atom::Atom;
    use crate::entity::clause::{Clause, Fact};
    use crate::entity::expression::{Argument, ExpressionNode};

    #[test]
    fn atom_state_operation() {
        let mut state = AtomState::new();

        let identifier: Identifier = "atom".parse().unwrap();
        let res = state.insert(identifier.clone()).unwrap();
        assert_eq!(res.identifier().to_string(), "atom");
        assert_eq!(&res, state.get(&identifier).unwrap());

        let res = state.insert(identifier);
        assert_eq!(res, Err(InsertError::Duplicated));
    }

    #[test]
    fn predicate_storeage_operation() {
        let mut state = PredicateState::new();

        let identifier: Identifier = "pred".parse().unwrap();
        let res = state.insert(identifier.clone(), 2).unwrap();
        assert_eq!(res.identifier().to_string(), "pred");
        assert_eq!(res.signature().arity(), 2);
        assert_eq!(&res, state.get(&identifier).unwrap());

        let res = state.insert(identifier.clone(), 2);
        assert_eq!(res, Err(InsertError::Duplicated));

        let res = state.insert(identifier.clone(), 1);
        assert_eq!(res, Err(InsertError::Conflict));
        assert_eq!(state.get(&identifier).unwrap().signature().arity(), 2);
    }

    #[test]
    fn predicate_domain_operation() {
        let mut atom_state = AtomState::new();
        let mut predicate_state = PredicateState::new();
        let predicate = predicate_state.insert("pred".parse().unwrap(), 1).unwrap();
        assert!(predicate_state.domain_insert(
            &predicate,
            0,
            atom_state.insert("a".parse().unwrap()).unwrap()
        ));
        assert!(predicate_state.domain_insert(
            &predicate,
            0,
            atom_state.insert("b".parse().unwrap()).unwrap()
        ));
        assert!(!predicate_state.domain_insert(
            &predicate,
            1,
            atom_state.insert("c".parse().unwrap()).unwrap()
        ));
        assert_eq!(
            predicate_state
                .domain(&predicate)
                .unwrap()
                .get(0)
                .unwrap()
                .iter()
                .map(|a| a.to_string())
                .collect::<HashSet<_>>(),
            {
                let mut set = HashSet::new();
                set.insert("a".to_string());
                set.insert("b".to_string());
                set
            },
        );
        assert_eq!(predicate_state.domain(&predicate).unwrap().get(1), None);
    }

    #[test]
    fn clause_state_operation() {
        let mut atom_state = AtomState::new();
        let mut predicate_state = PredicateState::new();

        let mut new_fact = |pred, args: &[&str]| {
            Fact::try_new(
                ExpressionNode::try_new_predicate(
                    pred,
                    args.iter()
                        .map(|arg| arg.parse().unwrap())
                        .map(|arg| atom_state.insert(arg).unwrap())
                        .map(|arg| Argument::Constant(arg))
                        .collect(),
                    false,
                )
                .unwrap(),
            )
            .unwrap()
            .into()
        };

        let pred1 = predicate_state.insert("pred1".parse().unwrap(), 2).unwrap();
        let pred2 = predicate_state.insert("pred2".parse().unwrap(), 1).unwrap();
        let mut state = ClauseState::new();

        state.insert(&pred1, new_fact(pred1.clone(), &["a", "b"]));
        state.insert(&pred1, new_fact(pred1.clone(), &["c", "d"]));
        state.insert(&pred2, new_fact(pred2.clone(), &["x"]));
        state.insert(&pred2, new_fact(pred2.clone(), &["y"]));

        let map_args = |fact: &ConcreteClause| {
            fact.arguments()
                .iter()
                .map(|arg| arg.to_string())
                .collect::<Vec<_>>()
        };

        let get_args = |pred| {
            state
                .get(&pred)
                .unwrap()
                .iter()
                .map(map_args)
                .collect::<Vec<_>>()
        };

        assert_eq!(get_args(pred1), vec![vec!["a", "b"], vec!["c", "d"]]);
        assert_eq!(get_args(pred2), vec![vec!["x"], vec!["y"]]);
    }
}
