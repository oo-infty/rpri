use std::collections::HashMap;

use snafu::prelude::*;

use crate::entity::atom::{AtomDefinition, AtomHandle};
use crate::entity::base::{EntityId, Identifier};
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
    pub fn get(&mut self, identifier: &Identifier) -> Option<AtomHandle> {
        self.atom_id
            .get(identifier)
            .and_then(|&entity_id| self.atoms.get(entity_id))
            .cloned()
    }
}

/// A type that stores all predicates and their relavent data and manages their
/// `EntityId`s.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct PredicateState {
    predicates: Vec<PredicateHandle>,
    predicate_id: HashMap<Identifier, EntityId>,
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
                Ok(predicate)
            }
        }
    }

    /// Get the handle of an predicate named `identifier`.
    pub fn get(&mut self, identifier: &Identifier) -> Option<PredicateHandle> {
        self.predicate_id
            .get(identifier)
            .and_then(|&entity_id| self.predicates.get(entity_id))
            .cloned()
    }
}

#[derive(Debug, Clone, Snafu, PartialEq, Eq)]
pub enum InsertError {
    #[snafu(display("Value indexed by the same identifier already exists, but it's equal to the one to be inserted."))]
    Duplicated,
    #[snafu(display("Value indexed by the same identifier already exists, along with conflicts in other fields."))]
    Conflict,
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::entity::atom::Atom;

    #[test]
    fn atom_state_operation() {
        let mut state = AtomState::new();

        let identifier: Identifier = "atom".parse().unwrap();
        let res = state.insert(identifier.clone()).unwrap();
        assert_eq!(res.identifier().to_string(), "atom");
        assert_eq!(res, state.get(&identifier).unwrap());

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
        assert_eq!(res, state.get(&identifier).unwrap());

        let res = state.insert(identifier.clone(), 2);
        assert_eq!(res, Err(InsertError::Duplicated));

        let res = state.insert(identifier.clone(), 1);
        assert_eq!(res, Err(InsertError::Conflict));
        assert_eq!(state.get(&identifier).unwrap().signature().arity(), 2);
    }
}
