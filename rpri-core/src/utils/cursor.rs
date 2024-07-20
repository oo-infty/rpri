use std::cmp::Ordering;

use snafu::prelude::*;

/// A [`Cursor`] is a type that helps iterate over a collection, which
/// is capable of moving forward and backward simultaneously. Compared to
/// [`Iterator`], it concentrates more on flexible moving capability.
pub trait Cursor: Sized {
    type Item;
    type Position;

    /// Return the element to which this cursor points. `None` if the target
    /// is invalid.
    fn current(&self) -> Option<Self::Item>;

    /// Get the current position if it's valid, or indicates its error state.
    fn position(&self) -> Result<Self::Position, CursorError>;

    /// Move the cursor towrad a given [`Direction`] and return whether the new
    /// position is valid.
    fn move_toward(&mut self, direction: Direction) -> bool;

    /// Move the cursor forward and return whether the new position is valid.
    fn next(&mut self) -> bool {
        self.move_toward(Direction::Forward)
    }

    /// Move the cursor backward and return whether the new position is valid.
    fn prev(&mut self) -> bool {
        self.move_toward(Direction::Backward)
    }

    /// Wrap the cursor into an iterator which starts from the current position.
    /// This is a compaitable method to use a [`Cursor`] as an [`Iterator`].
    fn into_iter(self, direction: Direction) -> CursorIter<Self> {
        CursorIter::new(self, direction)
    }
}

/// The direction of a [`Cursor`]'s movement.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Direction {
    Forward,
    Backward,
}

/// A type that describes a [`Cursor`]'s invalid position.
#[derive(Debug, Clone, Snafu, PartialEq, Eq, Hash)]
pub enum CursorError {
    #[snafu(display("The index is out of the left bound."))]
    Underflow,
    #[snafu(display("The index is out of the right bound."))]
    Overflow,
}

/// A [`IndexCursor`] is a [`Cursor`] that is able to access elements randomly
/// by indices. The underlying collection is usually a data structure of
/// continous-memory layout.
pub trait IndexCursor: Cursor {
    /// Return the length of the underlying collection.
    fn len(&self) -> Self::Position;

    /// Set the cursor's target by index.
    fn set(&mut self, index: Self::Position) -> bool;
}

/// A wrapper of [`Cursor`] that adapts it to an [`Iterator`].
pub struct CursorIter<T: Cursor> {
    inner: T,
    direction: Direction,
}

impl<T: Cursor> CursorIter<T> {
    pub fn new(inner: T, direction: Direction) -> Self {
        Self { inner, direction }
    }
}

impl<T: Cursor> Iterator for CursorIter<T> {
    type Item = T::Item;

    fn next(&mut self) -> Option<Self::Item> {
        let res = self.inner.current();
        self.inner.move_toward(self.direction);
        res
    }
}

/// A [`Cursor`] that points to a single object.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SingleCursor<'a, T: 'a> {
    target: &'a T,
    index: isize,
}

impl<'a, T: 'a> SingleCursor<'a, T> {
    pub fn new(target: &'a T) -> Self {
        Self { target, index: 0 }
    }
}

impl<'a, T: 'a> Cursor for SingleCursor<'a, T> {
    type Item = &'a T;
    type Position = usize;

    fn current(&self) -> Option<Self::Item> {
        if self.index == 0 {
            Some(self.target)
        } else {
            None
        }
    }

    fn position(&self) -> Result<Self::Position, CursorError> {
        match self.index.cmp(&0) {
            Ordering::Less => Err(CursorError::Underflow),
            Ordering::Equal => Ok(0),
            Ordering::Greater => Err(CursorError::Overflow),
        }
    }

    fn move_toward(&mut self, direction: Direction) -> bool {
        self.index = match direction {
            Direction::Forward => isize::min(self.index + 1, 1),
            Direction::Backward => isize::max(self.index - 1, -1),
        };
        self.index == 0
    }
}

impl<'a, T: 'a> IndexCursor for SingleCursor<'a, T> {
    fn len(&self) -> Self::Position {
        1
    }

    fn set(&mut self, index: Self::Position) -> bool {
        self.index = (index as isize).clamp(-1, 1);
        self.index == 0
    }
}

/// A [`Cursor`] of a [`slice`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SliceCursor<'a, T: 'a> {
    target: &'a [T],
    index: isize,
}

impl<'a, T: 'a> SliceCursor<'a, T> {
    pub fn new(target: &'a [T]) -> Self {
        Self { target, index: 0 }
    }

    pub fn with_index(target: &'a [T], index: usize) -> Self {
        let mut cur = Self::new(&target);
        cur.set(index);
        cur
    }
}

impl<'a, T: 'a> Cursor for SliceCursor<'a, T> {
    type Item = &'a T;
    type Position = usize;

    fn current(&self) -> Option<Self::Item> {
        match self.index.cmp(&0) {
            Ordering::Less => None,
            _ => self.target.get(self.index as usize),
        }
    }

    fn position(&self) -> Result<Self::Position, CursorError> {
        ensure!(self.index >= 0, UnderflowSnafu);
        ensure!((self.index as usize) < self.target.len(), OverflowSnafu);
        Ok(self.index as Self::Position)
    }

    fn move_toward(&mut self, direction: Direction) -> bool {
        self.index = match direction {
            Direction::Forward => isize::min(self.index + 1, self.target.len() as isize),
            Direction::Backward => isize::max(self.index - 1, -1),
        };
        0 <= self.index && (self.index as usize) < self.target.len()
    }
}

impl<'a, T: 'a> IndexCursor for SliceCursor<'a, T> {
    fn len(&self) -> Self::Position {
        self.target.len()
    }

    fn set(&mut self, index: Self::Position) -> bool {
        self.index = index.clamp(0, self.target.len()) as isize;
        (self.index as usize) < self.target.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn single_cursor_operation() {
        let num = 42;
        let mut cur = SingleCursor::new(&num);
        assert_eq!(cur.len(), 1);
        assert_eq!(cur.position(), Ok(0));
        assert_eq!(cur.current(), Some(&42));

        assert!(!cur.prev());
        assert_eq!(cur.position(), Err(CursorError::Underflow));
        assert_eq!(cur.current(), None);

        assert!(!cur.set(2));
        assert_eq!(cur.position(), Err(CursorError::Overflow));
        assert_eq!(cur.current(), None);

        let mut iter = SingleCursor::new(&num)
            .into_iter(Direction::Forward)
            .map(|x| x + 1);
        assert_eq!(iter.next(), Some(43));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn vec_cursor_operation() {
        let v = vec![1, 2, 3, 4, 5];
        let mut cur = SliceCursor::new(&v);
        assert_eq!(cur.len(), 5);

        assert_eq!(cur.current(), Some(&1));
        assert!(cur.next());
        assert_eq!(cur.current(), Some(&2));
        assert!(cur.move_toward(Direction::Forward));
        assert_eq!(cur.current(), Some(&3));
        assert!(cur.prev());
        assert_eq!(cur.current(), Some(&2));
        assert!(cur.move_toward(Direction::Backward));
        assert_eq!(cur.current(), Some(&1));
        assert!(!cur.prev());
        assert_eq!(cur.current(), None);

        assert_eq!(cur.position(), Err(CursorError::Underflow));
        assert!(!cur.set(5));
        assert_eq!(cur.position(), Err(CursorError::Overflow));
        assert!(cur.set(4));
        assert_eq!(cur.position(), Ok(4));
    }

    #[test]
    fn vec_cursor_iter() {
        let v = vec![1, 2, 3, 4, 5];

        let mut cur = SliceCursor::new(&v);
        cur.set(4);
        let mut iter = cur.into_iter(Direction::Backward);
        assert_eq!(iter.next(), Some(&5));
        assert_eq!(iter.next(), Some(&4));
        assert_eq!(iter.next(), Some(&3));
        assert_eq!(iter.next(), Some(&2));
        assert_eq!(iter.next(), Some(&1));
        assert_eq!(iter.next(), None);

        let res = SliceCursor::new(&v)
            .into_iter(Direction::Forward)
            .filter(|&x| *x % 2 == 1)
            .map(|&x| x * x)
            .sum::<i32>();
        assert_eq!(res, 35);
    }
}
