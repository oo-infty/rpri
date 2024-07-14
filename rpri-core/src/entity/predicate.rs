use std::fmt::{Display, Formatter, Result as FmtResult};
use std::sync::Arc;

use crate::entity::base::{EntityId, Identifier};

/// Type that represents predicates in Prolog.
pub trait Predicate: Clone + Eq + PartialEq + Display {
    fn id(&self) -> EntityId;

    fn identifier(&self) -> &Identifier;

    fn signature(&self) -> &Signature;
}

/// Structure of a predicate.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Signature {
    identifier: Identifier,
    arity: usize,
}

impl Signature {
    pub fn new(identifier: Identifier, arity: usize) -> Self {
        Self { identifier, arity }
    }

    pub fn identifier(&self) -> &Identifier {
        &self.identifier
    }

    pub fn arity(&self) -> usize {
        self.arity
    }
}

impl Display for Signature {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{}/{}", self.identifier, self.arity)
    }
}

#[derive(Debug, Clone)]
pub struct PredicateDefinition {
    signature: Signature,
    entity_id: EntityId,
}

impl PredicateDefinition {
    pub fn new(signature: Signature, entity_id: EntityId) -> Self {
        Self {
            signature,
            entity_id,
        }
    }
}

impl Predicate for PredicateDefinition {
    fn id(&self) -> EntityId {
        self.entity_id
    }

    fn identifier(&self) -> &Identifier {
        self.signature.identifier()
    }

    fn signature(&self) -> &Signature {
        &self.signature
    }
}

impl PartialEq for PredicateDefinition {
    /// Required that two predicates of the same ID have the same value.
    fn eq(&self, other: &Self) -> bool {
        if self.entity_id == other.entity_id {
            debug_assert_eq!(self.signature, other.signature);
        }
        self.entity_id == other.entity_id
    }
}

impl Eq for PredicateDefinition {}

impl Display for PredicateDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        self.signature.fmt(f)
    }
}

// Type that points to an `PredicateDefinition` and behaves as an `Predicate`.
#[derive(Debug, Clone)]
pub struct PredicateHandle {
    target: Arc<PredicateDefinition>,
}

impl Predicate for PredicateHandle {
    fn id(&self) -> EntityId {
        self.target.id()
    }

    fn identifier(&self) -> &Identifier {
        self.target.identifier()
    }

    fn signature(&self) -> &Signature {
        self.target.signature()
    }
}

impl From<PredicateDefinition> for PredicateHandle {
    fn from(value: PredicateDefinition) -> Self {
        Self {
            target: Arc::new(value),
        }
    }
}

impl PartialEq for PredicateHandle {
    /// Required that two predicates of the same ID have the same signature.
    fn eq(&self, other: &Self) -> bool {
        self.target == other.target
    }
}

impl Eq for PredicateHandle {}

impl Display for PredicateHandle {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        self.target.fmt(f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn signature_display() {
        let signature = Signature::new("pred".parse().unwrap(), 1);
        assert_eq!(signature.to_string(), "pred/1");

        let signature_quoted_identifier = Signature::new("'a pred'".parse().unwrap(), 2);
        assert_eq!(signature_quoted_identifier.to_string(), "'a pred'/2");
    }

    #[test]
    fn predicate_definition_equality() {
        let predicate1 =
            PredicateDefinition::new(Signature::new("predicate".parse().unwrap(), 1), 42);
        let predicate2 =
            PredicateDefinition::new(Signature::new("predicate".parse().unwrap(), 1), 42);
        assert_eq!(predicate1, predicate2);

        let predicate1 =
            PredicateDefinition::new(Signature::new("predicate1".parse().unwrap(), 1), 42);
        let predicate2 =
            PredicateDefinition::new(Signature::new("predicate2".parse().unwrap(), 1), 43);
        assert_ne!(predicate1, predicate2);

        let predicate1 =
            PredicateDefinition::new(Signature::new("predicate".parse().unwrap(), 1), 42);
        let predicate2 =
            PredicateDefinition::new(Signature::new("predicate".parse().unwrap(), 2), 43);
        assert_ne!(predicate1, predicate2);
    }

    #[test]
    fn predicate_handle_build() {
        let value = "predicate".parse().unwrap();
        let entity_id = 42;
        let signature = Signature::new(value, 1);
        let predicate = PredicateDefinition::new(signature.clone(), entity_id);

        let handle1 = PredicateHandle::from(predicate);
        assert_eq!(handle1.signature(), &signature);
        assert_eq!(handle1.id(), entity_id);

        let handle2 = handle1.clone();
        assert_eq!(handle2.signature(), &signature);
        assert_eq!(handle2.id(), entity_id);
    }

    #[test]
    fn predicate_handle_equality() {
        let handle1 = PredicateHandle::from(PredicateDefinition::new(
            Signature::new("predicate".parse().unwrap(), 1),
            42,
        ));
        let handle2 = handle1.clone();
        assert_eq!(handle1, handle2);

        let handle3 = PredicateHandle::from(PredicateDefinition::new(
            Signature::new("predicate2".parse().unwrap(), 1),
            43,
        ));
        assert_ne!(handle1, handle3);
        assert_ne!(handle2, handle3);
    }
}
