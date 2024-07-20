// use crate::entity::clause::{Clause, ConcreteClause, Fact, Rule};
use crate::entity::expression::{Argument, Expression, PredicateView};
// use crate::runtime::state::ClauseState;

/// An abstract form of SLD Resolution procedure. With the reference to the
/// expression tree, it understands the order of resolution (pre-order traversal
/// of the expression tree) and the predicate or expression component to
/// evaluate. It's implemented in a form similar to an double-direction iterator.
pub trait Flow: Clone {
    /// Proceed the traversal.
    fn step(&mut self);

    /// Undo the traversal.
    fn backtrack(&mut self);

    /// Return the current procedure to deal with. `None` if either traversal or
    /// backtrack is ended.
    fn current(&self) -> Option<FlowUnit>;
}

/// A type that represents the basic procedure of the resolution.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FlowUnit<'a> {
    Predicate {
        predicate: PredicateView<'a>,
        scope: usize,
    },
    Clause {
        arguments: &'a Vec<Argument>,
        negated: bool,
        scope: usize,
    },
    Connective {
        scope: usize,
    },
}

impl FlowUnit<'_> {
    pub fn arguments(&self) -> Option<&Vec<Argument>> {
        match self {
            Self::Predicate { predicate, .. } => Some(predicate.arguments()),
            Self::Clause { arguments, .. } => Some(arguments),
            Self::Connective { .. } => None,
        }
    }

    pub fn scope(&self) -> usize {
        match self {
            Self::Predicate { scope, .. } => *scope,
            Self::Clause { scope, .. } => *scope,
            Self::Connective { scope, .. } => *scope,
        }
    }
}

// #[derive(Debug, Clone, PartialEq, Eq)]
// enum ExpressionCollection<'a> {
//     Expression(ExpressionView<'a>),
//     Clause(&'a ConcreteClause),
// }

// impl<'a> From<ExpressionView<'a>> for ExpressionCollection<'a> {
//     fn from(value: ExpressionView<'a>) -> Self {
//         Self::Expression(value)
//     }
// }

// impl<'a> From<&'a ConcreteClause> for ExpressionCollection<'a> {
//     fn from(value: &'a ConcreteClause) -> Self {
//         Self::Clause(value)
//     }
// }

// #[derive(Debug, Clone)]
// enum CollectionIter<'a> {
//     SubExpression(Iter<'a, ExpressionView<'a>>),
//     Clause(Iter<'a, ConcreteClause>),
// }

// impl<'a> Iterator for CollectionIter<'a> {
//     type Item = ExpressionCollection<'a>;

//     fn next(&mut self) -> Option<Self::Item> {
//         match self {
//             CollectionIter::SubExpression(_) => todo!(),
//             CollectionIter::Clause(_) => todo!(),
//         }
//     }
// }

// impl<'a> From<Iter<'a, ExpressionView<'a>>> for CollectionIter<'a> {
//     fn from(value: Iter<'a, ExpressionView<'a>>) -> Self {
//         Self::SubExpression(value)
//     }
// }

// impl<'a> From<Iter<'a, ConcreteClause>> for CollectionIter<'a> {
//     fn from(value: Iter<'a, ConcreteClause>) -> Self {
//         Self::Clause(value)
//     }
// }

// #[derive(Debug, Clone)]
// struct StackFrame<'a> {
//     expression: ExpressionCollection<'a>,
//     iter: CollectionIter<'a>,
//     scope: usize,
// }

// #[derive(Debug, Clone)]
// pub struct ConcreteFlow<'a> {
//     clause_state: &'a ClauseState,
//     root: ExpressionView<'a>,
//     traversal_stack: Vec<StackFrame<'a>>,
//     choice_points: Vec<CollectionIter<'a>>,
// }

// impl<'a> ConcreteFlow<'a> {
//     pub fn new(clause_state: &'a ClauseState, root: ExpressionView<'a>) -> Self {
//         Self {
//             clause_state,
//             root,
//             traversal_stack: Vec::new(),
//             choice_points: Vec::new(),
//         }
//     }
// }

// impl<'a> Flow for ConcreteFlow<'a> {
//     fn step(&mut self) {
//         let Some(frame) = self.traversal_stack.last() else {
//             self.traversal_stack.push(StackFrame {
//                 expression: self.root.clone().into(),
//                 iter: self.root.iter(),
//                 scope: 0,
//             });
//             return;
//         };
//         todo!()
//     }

//     fn backtrack(&mut self) {
//         todo!()
//     }

//     fn current(&self) -> Option<FlowUnit> {
//         todo!()
//     }
// }
