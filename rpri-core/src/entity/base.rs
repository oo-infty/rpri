use std::fmt::{Display, Error as FmtError, Formatter};
use std::str::FromStr;

/// Type that distinguish entities from each other.
pub type EntityId = usize;

/// A valid name of atoms or predicates.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Identifier {
    value: String,
    quoted: bool,
}

impl Identifier {
    fn new(value: String, quoted: bool) -> Self {
        Self { value, quoted }
    }

    pub fn inner(&self) -> &str {
        &self.value
    }
}

impl FromStr for Identifier {
    type Err = ();

    /// Parse a Prolog identifier from string. A valid Prolog identifier should
    /// only contains alphabets, digits and underscores with a lowercase char or
    /// underscore as head, otherwise quotes are needed.
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let chars: Vec<_> = s.chars().collect();

        let (Some(&first), Some(&last)) = (chars.first(), chars.last()) else {
            return Err(());
        };

        // Check whether it's quoted.
        if chars.len() < 2 || (first != '\'' && last != '\'') {
            if !first.is_ascii_lowercase() && first != '_' {
                Err(())
            } else if s.chars().any(|c| !c.is_ascii_alphanumeric() && c != '_') {
                Err(())
            } else {
                Ok(Self::new(s.into(), false))
            }
        } else {
            // Get inner content.
            let Some(chars) = chars.as_slice().get(1..(chars.len() - 1)) else {
                return Err(());
            };

            let iter = chars.iter().rev();
            // Occurrence of a single quote not followed by a backslash inside a
            // pair of quotes is invalid.
            let invalid_quote = iter
                .clone()
                .zip(iter.skip(1))
                .any(|(&current, &next)| current == '\'' && next != '\\')
                || chars.first().is_some_and(|&c| c == '\'');

            if invalid_quote {
                Err(())
            } else {
                Ok(Self::new(chars.iter().collect(), true))
            }
        }
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), FmtError> {
        if self.quoted {
            write!(f, "'{}'", self.value)
        } else {
            write!(f, "{}", self.value)
        }
    }
}

/// A valid name of a variable.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VariableIdentifier(String);

impl VariableIdentifier {
    pub fn inner(&self) -> &str {
        &self.0
    }
}

impl FromStr for VariableIdentifier {
    type Err = ();

    /// Parse a Prolog identifier from string for a variable. A valid Prolog
    /// identifier should for a variable only contains alphabets, digits and
    /// underscores with a uppercase char or underscore as head.
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let Some(first) = s.chars().next() else {
            return Err(());
        };

        if !first.is_ascii_uppercase() {
            Err(())
        } else if s.chars().any(|c| !c.is_ascii_alphanumeric() && c != '_') {
            Err(())
        } else {
            Ok(Self(s.into()))
        }
    }
}

impl Display for VariableIdentifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn identifier_parse() {
        assert_eq!("atom".parse(), Ok(Identifier::new("atom".into(), false)));
        assert_eq!("_atom".parse(), Ok(Identifier::new("_atom".into(), false)));
        assert_eq!("a1".parse(), Ok(Identifier::new("a1".into(), false)));
        assert_eq!(r"' \''".parse(), Ok(Identifier::new(r" \'".into(), true)));
        assert_eq!("''".parse(), Ok(Identifier::new("".into(), true)));
        assert_eq!("".parse(), Err::<Identifier, _>(()));
        assert_eq!("Atom".parse(), Err::<Identifier, _>(()));
        assert_eq!("1Atom".parse(), Err::<Identifier, _>(()));
        assert_eq!(r"' ' '".parse(), Err::<Identifier, _>(()));
        assert_eq!("'".parse(), Err::<Identifier, _>(()));
    }

    #[test]
    fn identifier_display() {
        let atom = "atom".parse::<Identifier>().unwrap();
        assert_eq!(atom.to_string(), "atom");

        let atom_with_spaces = "'atom with spaces'".parse::<Identifier>().unwrap();
        assert_eq!(atom_with_spaces.to_string(), "'atom with spaces'");
    }
}
