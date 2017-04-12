use std::collections::VecDeque;
use std::iter::Fuse;

/// An iterator with a `peekn` method allowing for arbitrary lookahead.
pub struct MultiPeek<I> where I: Iterator {
    iter: Fuse<I>,
    buf: VecDeque<I::Item>,
}

/// Creates an iterator with allowing for arbitrary lookahead.
pub fn multipeek<I>(iterable: I) -> MultiPeek<I::IntoIter> where I: IntoIterator {
    MultiPeek {
        iter: iterable.into_iter().fuse(),
        buf: VecDeque::new(),
    }
}

impl<I> MultiPeek<I> where I: Iterator {
    /// Returns a reference to the next value without advancing the iterator.
    pub fn peek(&mut self) -> Option<&I::Item> {
        self.peekn(0)
    }

    /// Returns a reference to the upcoming nth value without advancing the iterator.
    ///
    /// Note that this is zero-indexed, i.e. `peekn(0)` is the same as calling `peek`.
    pub fn peekn(&mut self, n: usize) -> Option<&I::Item> {
        let len = self.buf.len();

        for _ in len..(n + 1) {
            match self.iter.next() {
                Some(x) => self.buf.push_back(x),
                None => return None,
            }
        }
        return Some(&self.buf[n]);
    }
}

impl<I> Iterator for MultiPeek<I> where I: Iterator {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        if self.buf.is_empty() {
            self.iter.next()
        } else {
            self.buf.pop_front()
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let (lb, ub) = self.iter.size_hint();
        (lb, ub.map(|sh| sh + self.buf.len()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_peeking_too_far() {
        let mut iter = multipeek(Some(1));

        assert_eq!(iter.peek(), Some(&1));
        assert_eq!(iter.peekn(1), None);
        assert_eq!(iter.peekn(10), None);
        assert_eq!(iter.peekn(100), None);

        assert_eq!(iter.next(), Some(1));

        assert_eq!(iter.peek(), None);
        assert_eq!(iter.peekn(1), None);
        assert_eq!(iter.peekn(10), None);
        assert_eq!(iter.peekn(100), None);
    }

    #[test]
    fn test_multipeek() {
        let mut iter = multipeek(1..6);

        assert_eq!(iter.peek(), Some(&1));
        assert_eq!(iter.peekn(1), Some(&2));
        assert_eq!(iter.peekn(2), Some(&3));

        assert_eq!(iter.next(), Some(1));
        assert_eq!(iter.peek(), Some(&2));
        assert_eq!(iter.peekn(1), Some(&3));

        assert_eq!(iter.next(), Some(2));
        assert_eq!(iter.next(), Some(3));
        assert_eq!(iter.peek(), Some(&4));
        assert_eq!(iter.peekn(1), Some(&5));

        assert_eq!(iter.peekn(2), None);
        assert_eq!(iter.peekn(10), None);
    }
}
