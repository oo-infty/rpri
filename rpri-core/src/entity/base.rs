use std::fmt::{Display, Error as FmtError, Formatter};

/// Type that distinguish entities from each other.
pub type EntityId = usize;

/// A valid name of atoms or predicates.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Identifier(String);

impl Identifier {
    pub fn inner(&self) -> &str {
        &self.0
    }
}

impl From<String> for Identifier {
    fn from(value: String) -> Self {
        Self(value)
    }
}

impl From<&str> for Identifier {
    fn from(value: &str) -> Self {
        Self(value.to_string())
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), FmtError> {
        let valid_char = |c: char| c.is_ascii_alphanumeric() || c == '_';

        if self.0.is_empty() {
            write!(f, "''")
        } else if self.0.chars().any(|c| !valid_char(c)) {
            write!(f, "'{}'", self.0)
        } else if let Some(c) = self.0.chars().next() {
            if c.is_ascii_digit() || c.is_uppercase() {
                write!(f, "'{}'", self.0)
            } else {
                write!(f, "{}", self.0)
            }
        } else {
            unreachable!();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn identifier_display() {
        let atom = Identifier::from("atom");
        assert_eq!(atom.to_string(), "atom");

        let atom_empty = Identifier::from("");
        assert_eq!(atom_empty.to_string(), "''");

        let atom_with_spaces = Identifier::from("atom with spaces");
        assert_eq!(atom_with_spaces.to_string(), "'atom with spaces'");

        let atom_string = Identifier::from("*an-atom*");
        assert_eq!(atom_string.to_string(), "'*an-atom*'");

        let atom_starts_with_digit = Identifier::from("0atom");
        assert_eq!(atom_starts_with_digit.to_string(), "'0atom'");

        let atom_starts_with_uppercase = Identifier::from("Atom");
        assert_eq!(atom_starts_with_uppercase.to_string(), "'Atom'");
    }
}
