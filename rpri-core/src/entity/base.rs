use std::fmt::{Display, Error as FmtError, Formatter};
use std::str::FromStr;

use snafu::prelude::*;

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
    type Err = ParseIdentifierError;

    /// Parse a Prolog identifier from string. A valid Prolog identifier should
    /// only contains alphabets, digits and underscores with a lowercase char or
    /// underscore as head, otherwise quotes are needed.
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let chars: Vec<_> = s.chars().collect();

        let (Some(&first), Some(&last)) = (chars.first(), chars.last()) else {
            return Err(ParseIdentifierError::Empty);
        };

        // Check whether it's quoted.
        if chars.len() < 2 || (first != '\'' && last != '\'') {
            ensure!(
                first.is_ascii_lowercase() || first == '_',
                InvalidFirstI { input_string: s },
            );
            ensure!(
                s.chars().all(|c| c.is_ascii_alphanumeric() || c == '_'),
                InvalidCharacterI { input_string: s },
            );
            Ok(Self::new(s.into(), false))
        } else {
            // Get inner content.
            let chars = chars
                .as_slice()
                .get(1..(chars.len() - 1))
                .with_context(|| InvalidCharacterI { input_string: s })?;

            let iter = chars.iter().rev();
            // Occurrence of a single quote not followed by a backslash inside a
            // pair of quotes is invalid.
            let invalid_quote = iter
                .clone()
                .zip(iter.skip(1))
                .any(|(&current, &next)| current == '\'' && next != '\\')
                || chars.first().is_some_and(|&c| c == '\'');

            ensure!(!invalid_quote, UnexpectedQuoteI { input_string: s });
            Ok(Self::new(chars.iter().collect(), true))
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

#[derive(Debug, Snafu, PartialEq, Eq)]
#[snafu(context(suffix(I)))]
pub enum ParseIdentifierError {
    #[snafu(display("Could not parse an empty string to an identifier. Got: ``."))]
    Empty,
    #[snafu(display("An unquoted identifier should only contain alphabets, digits and underscores. Got: `{input_string}`."))]
    InvalidCharacter { input_string: String },
    #[snafu(display("An unquoted identifier should starts with an lowercase ASCII character or unerscore. Got: `{input_string}`."))]
    InvalidFirst { input_string: String },
    #[snafu(display(
        "An unexpected quote occurred within quote delimiters. Got: `{input_string}`."
    ))]
    UnexpectedQuote { input_string: String },
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
    type Err = ParseVariableError;

    /// Parse a Prolog identifier from string for a variable. A valid Prolog
    /// identifier should for a variable only contains alphabets, digits and
    /// underscores with a uppercase char or underscore as head.
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let first = s.chars().next().with_context(|| EmptyV)?;
        ensure!(
            first.is_ascii_uppercase() || first == '_',
            InvalidFirstV { input_string: s }
        );
        ensure!(
            s.chars().all(|c| c.is_ascii_alphanumeric() || c == '_'),
            InvalidCharacterV { input_string: s },
        );
        Ok(Self(s.into()))
    }
}

impl Display for VariableIdentifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Debug, Snafu, PartialEq, Eq)]
#[snafu(context(suffix(V)))]
pub enum ParseVariableError {
    #[snafu(display("Could not parse an empty string to a variable identifier. Got: ``."))]
    Empty,
    #[snafu(display("An unquoted variable identifier should only contain alphabets, digits and underscores. Got: `{input_string}`."))]
    InvalidCharacter { input_string: String },
    #[snafu(display("An unquoted variable identifier should starts with an uppercase ASCII character or unerscore. Got: `{input_string}`."))]
    InvalidFirst { input_string: String },
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
        assert!(matches!(
            "".parse(),
            Err::<Identifier, _>(ParseIdentifierError::Empty)
        ));
        assert!(matches!(
            "Atom".parse(),
            Err::<Identifier, _>(ParseIdentifierError::InvalidFirst { input_string: _ })
        ));
        assert!(matches!(
            "1Atom".parse(),
            Err::<Identifier, _>(ParseIdentifierError::InvalidFirst { input_string: _ })
        ));
        assert!(matches!(
            "' ' '".parse(),
            Err::<Identifier, _>(ParseIdentifierError::UnexpectedQuote { input_string: _ })
        ));
        assert!(matches!(
            "'".parse(),
            Err::<Identifier, _>(ParseIdentifierError::InvalidFirst { input_string: _ })
        ));
    }

    #[test]
    fn identifier_display() {
        let atom = "atom".parse::<Identifier>().unwrap();
        assert_eq!(atom.to_string(), "atom");

        let atom_with_spaces = "'atom with spaces'".parse::<Identifier>().unwrap();
        assert_eq!(atom_with_spaces.to_string(), "'atom with spaces'");
    }

    #[test]
    fn variable_identifier_parse() {
        assert_eq!("X".parse(), Ok(VariableIdentifier("X".to_string())));
        assert_eq!("_X".parse(), Ok(VariableIdentifier("_X".to_string())));
        assert!(matches!(
            "".parse(),
            Err::<VariableIdentifier, _>(ParseVariableError::Empty)
        ));
        assert!(matches!(
            "var".parse(),
            Err::<VariableIdentifier, _>(ParseVariableError::InvalidFirst { input_string: _ })
        ));
        assert!(matches!(
            "Var-x".parse(),
            Err::<VariableIdentifier, _>(ParseVariableError::InvalidCharacter { input_string: _ })
        ));
    }
}
