use std::fmt::{Display, Formatter, Result as FmtResult};
use std::hash::{Hash, Hasher};
use std::sync::Arc;

use crate::entity::base::{EntityId, Identifier};

/// Trait of the fundamental element in Prolog.
pub trait Atom: Clone + Eq + PartialEq + Display + Hash {
    fn id(&self) -> EntityId;

    fn identifier(&self) -> &Identifier;
}

/// Type that stores all relavent data of an atom in one place.
#[derive(Debug, Clone)]
pub struct AtomDefinition {
    identifier: Identifier,
    entity_id: EntityId,
}

impl AtomDefinition {
    pub fn new(identifier: Identifier, entity_id: EntityId) -> Self {
        Self {
            identifier,
            entity_id,
        }
    }
}

impl Atom for AtomDefinition {
    fn id(&self) -> EntityId {
        self.entity_id
    }

    fn identifier(&self) -> &Identifier {
        &self.identifier
    }
}

impl PartialEq for AtomDefinition {
    /// Required that two atoms of the same ID have the same identifier.
    fn eq(&self, other: &Self) -> bool {
        if self.entity_id == other.entity_id {
            debug_assert_eq!(self.identifier, other.identifier);
        }
        self.entity_id == other.entity_id
    }
}

impl Eq for AtomDefinition {}

impl Display for AtomDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        self.identifier.fmt(f)
    }
}

impl Hash for AtomDefinition {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_usize(self.entity_id);
    }
}

// Type that points to an `AtomDefinition` and behaves as an `Atom`.
#[derive(Debug, Clone, Hash)]
pub struct AtomHandle {
    target: Arc<AtomDefinition>,
}

impl Atom for AtomHandle {
    fn id(&self) -> EntityId {
        self.target.id()
    }

    fn identifier(&self) -> &Identifier {
        self.target.identifier()
    }
}

impl From<AtomDefinition> for AtomHandle {
    fn from(value: AtomDefinition) -> Self {
        Self {
            target: Arc::new(value),
        }
    }
}

impl PartialEq for AtomHandle {
    fn eq(&self, other: &Self) -> bool {
        self.target == other.target
    }
}

impl Eq for AtomHandle {}

impl Display for AtomHandle {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        self.target.fmt(f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn atom_definition_equality() {
        let atom1 = AtomDefinition::new("atom".parse().unwrap(), 42);
        let atom2 = AtomDefinition::new("atom".parse().unwrap(), 42);
        assert_eq!(atom1, atom2);

        let atom1 = AtomDefinition::new("atom1".parse().unwrap(), 42);
        let atom2 = AtomDefinition::new("atom2".parse().unwrap(), 43);
        assert_ne!(atom1, atom2);
    }

    #[test]
    fn atom_handle_build() {
        let identifier = "atom".parse::<Identifier>().unwrap();
        let entity_id = 42;
        let atom = AtomDefinition::new(identifier.clone(), entity_id);

        let handle1 = AtomHandle::from(atom);
        assert_eq!(handle1.identifier(), &identifier);
        assert_eq!(handle1.id(), entity_id);

        let handle2 = handle1.clone();
        assert_eq!(handle2.identifier(), &identifier);
        assert_eq!(handle2.id(), entity_id);
    }

    #[test]
    fn atom_handle_equality() {
        let handle1 = AtomHandle::from(AtomDefinition::new("atom".parse().unwrap(), 42));
        let handle2 = handle1.clone();
        assert_eq!(handle1, handle2);

        let handle3 = AtomHandle::from(AtomDefinition::new("atom2".parse().unwrap(), 43));
        assert_ne!(handle1, handle3);
        assert_ne!(handle2, handle3);
    }
}
