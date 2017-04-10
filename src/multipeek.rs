use std::collections::VecDeque;
use std::iter::Fuse;

pub struct MultiPeek<I> where I: Iterator {
    iter: Fuse<I>,
    buf: VecDeque<I::Item>,
    index: usize,
}

pub fn multipeek<I>(iterable: I) -> MultiPeek<I::IntoIter> where I: IntoIterator {
    MultiPeek {
        iter: iterable.into_iter().fuse(),
        buf: VecDeque::new(),
        index: 0,
    }
}

impl<I> MultiPeek<I> where I: Iterator {
    pub fn reset_peek(&mut self) {
        self.index = 0;
    }
}

impl<I> MultiPeek<I> where I: Iterator {
    pub fn peek(&mut self) -> Option<&I::Item> {
        let ret = if self.index < self.buf.len() {
            Some(&self.buf[self.index])
        } else {
            match self.iter.next() {
                Some(x) => {
                    self.buf.push_back(x);
                    Some(&self.buf[self.index])
                },
                None => {
                    return None
                }
            }
        };
        self.index += 1;
        ret
    }
}

impl<I> Iterator for MultiPeek<I> where I: Iterator {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.index = 0;
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
